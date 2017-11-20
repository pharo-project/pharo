Class {
	#name : #GLMMorphicActionRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#category : #Glamour-Morphic-Renderer
}

{ #category : #accessing }
GLMMorphicActionRenderer >> actionArguments [
	^ {  }
]

{ #category : #accessing }
GLMMorphicActionRenderer >> actionSelector [
	^ #morphicActOn:
]

{ #category : #rendering }
GLMMorphicActionRenderer >> render: anAction [
	|b|
	b := GLMPluggableButtonMorph
			on: anAction getState: nil action: self actionSelector.
	b
		theme: UITheme current;
		arguments: (self actionArguments ifNil: [{b}]);
		cornerStyle: (UITheme current buttonCornerStyleIn: nil);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		getEnabledSelector: nil;
		setBalloonText: ((anAction help ifNil: [ anAction title]) , Character tab asString , anAction shortcutAsString) trimBoth;
		extent: b minExtent;
		removeProperty: #theme.
	anAction shouldShowTitle 
		ifTrue: [ b 
				icon: anAction icon ;
				label: anAction title font: UITheme current buttonFont ]
		ifFalse: [ b label: (AlphaImageMorph new image: anAction icon) ].
	
	"this is a hack to tell the GLMUITheme to not draw the border and the fill, and to disable the focus"
	b 
		valueOfProperty: #noBorder ifAbsentPut: [ true ];
		valueOfProperty: #noFill ifAbsentPut: [ true ];
		setProperty: #wantsKeyboardFocusNavigation toValue: false;
		borderWidth: 0.
	^ b	
]
