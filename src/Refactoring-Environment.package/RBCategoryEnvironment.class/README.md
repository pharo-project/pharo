I am a RBBrowserEnvironment on a set of category names.
I containt all entities using this category name.
I am more restricted to the exact category name compared
to a package environment.

Example, all Morph subclasses in category Morphic-Base-Menus

(RBBrowserEnvironment new forClasses: Morph withAllSubclasses) forCategories: {#'Morphic-Base-Menus'}