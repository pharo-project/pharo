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
	Font { #name: "Source Sans Pro", #size: 10 },
	Geometry { #height: 25 },
	.label [
		Geometry { #hResizing: true },
		.headerError [Draw { #color:  Color{ #red: 1, #green: 0, #blue: 0, #alpha: 1}}  ],
		.headerSuccess [Draw { #color: Color{ #red: 0, #green: 1, #blue: 0, #alpha: 1}}  ],
		.header [ 
			Draw { #color: Color{ #rgb: 622413393 }},
			Font { #name: "Lucida Grande", #size: 10, #bold: true } ],
		.shortcut [ 
			Draw { #color: Color{ #rgb: 622413393 } },
			Font { #name: "Lucida Grande", #size: 10 }
		],
		.dim [
			Draw { #color : Color{ #rgb: 708480675 } }
		]
	],
	.link [  
		Geometry { #hResizing: true }		
	],
	.button [  
		Geometry { #width: 100 },
		.small [
		   Geometry { #width: 26 }
		]
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
		Container { 
			#borderColor: Color { #rgb: 0, #alpha: 0 }, 
			#borderWidth: 2, 
			#padding: 5 },
		Geometry { #width: 150, #height: 29, #hResizing: true, #vResizing: false }
	],
	.menuBar [
		Geometry { #width: 150, #hResizing: true }
	],
	.actionButton [  
		Geometry { #width: 60, #hResizing: false },
		.showIcon [ Geometry { #width: 25 } ]
	],
	.toolBar [ 
		Geometry { #hResizing: true },
		.icons [ 
			Geometry { #height: 30 }
		],
		.iconsAndLabel [  
			Geometry { #height: 45 }
		]
	],
	.code [
		Font { #name : "Source Code Pro", #size : 10 }
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

{ #category : #accessing }
SpStyle class >> reset [ 
	<script>

	DefaultStyleSheet := nil
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
