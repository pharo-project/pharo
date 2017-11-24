Class {
	#name : #GTInspectorHelp,
	#superclass : #CustomHelp,
	#category : #'GT-Inspector'
}

{ #category : #accessing }
GTInspectorHelp class >> bookName [ 
	^'GT Inspector'
]

{ #category : #documentation }
GTInspectorHelp class >> contact [
	^ HelpTopic 
		title: 'Contact'
		contents: 
'This Help has been written by Alexandre Bergel <alexandre.bergel AT me.com>. Feel free to get in touch. December 16, 2015.'
]

{ #category : #documentation }
GTInspectorHelp class >> extensions [
		
	^ HelpTopic 
		title: 'Extensions'
		contents: 'Inspector allows the developer to define custom presentations in different classes. These custom presentations take effect when the Inspector reaches an instance of the extended class. You can find below the list of extensions from this image.
		
You can also browse this list by inspecting:

		GTInspector inspectorExtendingMethods

', self extensionsString
]

{ #category : #documentation }
GTInspectorHelp class >> extensionsString [
	| methods title |
	^ String streamContents: [ :s |
		((GTInspector inspectorExtendingMethods groupedBy: #methodClass)
		associations sorted: [ :a :b | a key name < b key name ]) 
		do: [ :classMethodsAssociation |
			s nextPutAll: classMethodsAssociation key name; cr.
			methods := classMethodsAssociation value.
			methods do: [ :each | 
				title := each ast allChildren 
					detect: [ :node | node isMessage and: [ node selector = #title: ] ]
					ifFound: [ :node | node arguments first value. ]
					ifNone: [ each selector ].
				title isString ifFalse: [ title := each selector ].
				s 
					nextPutAll: '- '; 
					nextPutAll: title; 
					nextPutAll: ' ('; 
					nextPutAll: each selector; 
					nextPut: $);
					cr ].
			s cr ] ]
]

{ #category : #accessing }
GTInspectorHelp class >> key [
	^ self name
]

{ #category : #documentation }
GTInspectorHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'GT Inspector is a moldable inspector that is part of Pharo and was developed as part of the Glamorous Toolkit project (http://gt.moosetechnology.org)'
]

{ #category : #accessing }
GTInspectorHelp class >> pages [
	^#(overview table roassal extensions)
]

{ #category : #documentation }
GTInspectorHelp class >> roassal [
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
]

{ #category : #documentation }
GTInspectorHelp class >> table [
	^ HelpTopic 
		title: 'Table'
		contents: 
'A table is a presentation that simply list elements. For example, you can create a table presentation for the class OrderedCollection by defining the following method in OrderedCollection:
-=-=-=-=
gtInspectorTableIn: composite
	<gtInspectorPresentationOrder: -15>
	composite table
		title: ''Table'';
		display: [ self collect: #asString ]
-=-=-=-=

Inspect the expression:
((1 to: 5), (1 to: 5), (1 to: 5)) asOrderedCollection

You will have a list of numbers.

'
]
