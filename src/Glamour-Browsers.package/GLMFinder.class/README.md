A GLMFinder models a browsers that behaves like the Mac Finder: whenever the selection port is set on one pane, a new one is created to the right with the selection as entity. The Finder opens the first pane on the entity.

The Finder communicates with the Renderer 

Input ports:
- entity: this is passed to the first pane

Output ports:
- selection: this port is populated with the value from the last selection port from one of the panes