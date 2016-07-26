documentation
roassal
	^ HelpTopic 
		title: 'Roassal'
		contents: 
'Roassal is a visualization engine. GTInspector may uses Roassal to craft a visual representation for a given object.

First of all, you need to load Roassal. You can verify is Roassal is loaded why opening the World menu: a Roassal entry should be found. If no, then Roassal is not loaded. You can load Roassal2 from the Catalog browser.

You can then create a visual inspection by defining the following method in a class:
-=-=-=-=
gtInspectorViewIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: ''View'';
		initializeView: [ | v |
			v := RTView new.
			...
			v ]
-=-=-=-=

As an example, consider the following method you can defined on the class OrderedCollection:
-=-=-=-=
gtInspectorIdentityIn: composite
	<gtInspectorPresentationOrder: -10>
	composite roassal2
		title: ''Identity'';
		initializeView: [
			| b |
			b := RTMondrian new.
			b nodes: self.
			b normalizer distinctColor.
			b layout grid; gapSize: 1.
			b ]
-=-=-=-=

Inspecting the expression:
((1 to: 5), (1 to: 5), (1 to: 5)) asOrderedCollection

will give a color to each number.

You can give the method name you wish. The pragma: <gtInspectorPresentationOrder: -10> is essential as it is a marker for the GTInspector framework. -10 indicates the location of the tab. The tab ''Raw'' has a priority of 10. So giving a priority lower than 10 will position your visualization on the left of ''Raw''. Greater than 10 on the right of it.
'