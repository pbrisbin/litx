dist: dist/litx.tar.gz

ARCHIVE_TARGETS := \
  dist/litx/litx \
  dist/litx/completion/bash \
  dist/litx/completion/fish \
  dist/litx/completion/zsh \
  dist/litx/doc/litx.1 \
  dist/litx/Makefile

dist/litx.tar.gz: $(ARCHIVE_TARGETS)
	tar -C ./dist -czvf $@ ./litx

SRCS := $(shell \
  find ./src ./app -name '*.hs'; \
  echo stack.yaml; \
  echo litx.cabal \
)

dist/litx/litx: $(SRCS)
	mkdir -p ./dist/litx
	stack build --pedantic --test --copy-bins --local-bin-path dist/litx

dist/litx/completion/%: dist/litx/litx
	mkdir -p ./dist/litx/completion
	./$< --$(@F)-completion-script litx > dist/litx/completion/$(@F)

PANDOC ?= stack exec pandoc --

dist/litx/doc/%: doc/%.md
	mkdir -p ./dist/litx/doc
	$(PANDOC) --standalone $< --to man >$@

dist/litx/Makefile: Makefile
	cp $< $@

.PHONY: clean
clean:
	$(RM) -r ./dist
	stack clean --full

DESTDIR ?=
PREFIX ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

INSTALL ?= $(shell command -v ginstall 2>/dev/null || echo install)

.PHONY: install
install:
	$(INSTALL) -Dm755 litx $(DESTDIR)$(PREFIX)/bin/litx
	$(INSTALL) -Dm644 completion/bash $(DESTDIR)$(PREFIX)/share/bash-completion/completions/litx
	$(INSTALL) -Dm644 completion/fish $(DESTDIR)$(PREFIX)/share/fish/vendor_completions.d/litx.fish
	$(INSTALL) -Dm644 completion/zsh $(DESTDIR)$(PREFIX)/share/zsh/site-functions/_litx
	$(INSTALL) -Dm644 doc/litx.1 $(DESTDIR)$(MANPREFIX)/man1/litx.1

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(PREFIX)/bin/litx
	$(RM) $(DESTDIR)$(PREFIX)/share/bash-completion/completions/litx
	$(RM) $(DESTDIR)$(PREFIX)/share/fish/vendor_completions.d/litx.fish
	$(RM) $(DESTDIR)$(PREFIX)/share/zsh/site-functions/_litx
	$(RM) $(DESTDIR)$(MANPREFIX)/man1/litx.1

.PHONY: install.check
install.check: dist/litx.tar.gz
	cp dist/litx.tar.gz /tmp && \
	  cd /tmp && \
	  tar xvf litx.tar.gz && \
	  cd litx && \
	  make install PREFIX=$$HOME/.local
	PATH=$$HOME/.local/bin:$$PATH litx --help
