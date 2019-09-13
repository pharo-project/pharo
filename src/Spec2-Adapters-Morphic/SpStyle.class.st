"
I'm a style to be used in morphic adaptors. 
I can be a class (which is a set of classes and properties) or a property. 
My children will define concrete implementations of each.

Most important part, a style can be applied to (using #applyTo:) a morph. This morph will be the morph generated in an adaptor.
"
Class {
	#name : #SpStyle,
	#superclass : #Object,
	#classVars : [
		'DefaultStyleSheet'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #private }
SpStyle class >> createDefaultStyleSheet [

	^ SpStyleSTONReader fromString: '
.application [
	Font { #name : "Source Sans Pro", #size : 10 },
	Geometry { #height: 25 },
	.label [
		Geometry { #hResizing: true }		
	],
	.link [  
		Geometry { #hResizing: true }		
	],
	.button [  
		Geometry { #width: 25 }
	],
	.checkBox [  
		Geometry { #hResizing: true }
	],
	.radioButton [ 
		Geometry { #hResizing: true }
	],
	.dropList [ 
		Geometry { #width: 150, #hResizing: true }
	],
	.list [ 
		Geometry { #width: 150, #hResizing: true, #vResizing: true }
	],
	.slider [
		Geometry { #width: 150, #hResizing: true }
	],
	.actionBar [  
		Container { #borderWidth: 5, #padding: 5 },
		Geometry { #width: 150, #hResizing: true }
	],
	.menuBar [
		Geometry { #width: 150, #hResizing: true }
	],
	.actionButton [  
		Geometry { #width: 80, #hResizing: true }
	],
	.toolBar [ 
		Geometry { #hResizing: true },
		.icons [ 
			Geometry { #height: 30 }
		],
		.iconsAndLabel [  
			Geometry { #height: 45 }
		]
	]
]
'
]

{ #category : #accessing }
SpStyle class >> defaultStyleSheet [

	self flag: #TODO. "This is wrong. Default style sheet should reside in the Pharo application, 
	but since we do not have that abstraction (yet) in the system, we put it here for now"
	^ DefaultStyleSheet ifNil: [ 
		DefaultStyleSheet := self createDefaultStyleSheet ].

]

{ #category : #operations }
SpStyle >> applyTo: aMorph [

	self subclassResponsibility
]

{ #category : #testing }
SpStyle >> isStyleClass [

	^ false
]

{ #category : #testing }
SpStyle >> isStyleProperty [

	^ false
]
