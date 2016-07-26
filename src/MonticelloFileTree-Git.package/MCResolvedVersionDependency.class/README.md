This version dependency entry contains the specific version we depend on.

On normal use, upon encountering a dependency, Monticello will re-ask all repositories to load the dependency, instead of just the current repository.

In GitFileTree, due to the --depth parameter, version numbering may differ, so we have to work around Monticello for this scheme to work.

If Monticello was properly selecting packages per UUID, then it would probably not be necessary.