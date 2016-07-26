accessing
rationale
	^ 'Checks for the class name in a selector. This is redundant since to call the you must already refer to the class name. For example, openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser. Avoiding selector including class name gives a chance to have more polymorphic methods.'