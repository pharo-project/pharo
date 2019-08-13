"
AbstractBasicWidget is an abstract class for basic widgets

I collaborate with
- ValueHolder to managed some of my variables.
- ComposablePresenter to manage me with other elements.

I provide the following variables and their accessors
- enabled is a boolean representing if I am usable or not, it is true by default. It  have also shortcut enable and disable.
- help is the text displayed in the tooltip.
- borderWidth, its default value is 0, it must be called after have displayed the widget. 
- borderColor, its default value is transparent, it must be called after have displayed the widget. 

For the two methods borderWidth and borderColor, notice that some of my subclasses don't display my border

Me and my subclasses provide shortcut hook for my valueHolder, they are named ""when"" followed of the event.

My drag and drop system don't work well.

todo
- defaultColor
- color
- eventKeyStrokesForNextFocus

"
Class {
	#name : #AbstractWidgetPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'helpHolder',
		'borderWidth',
		'borderColor',
		'enabledHolder',
		'dragEnabled',
		'dropEnabled',
		'dragTransformationBlock',
		'wantDropBlock',
		'acceptDropBlock',
		'transferBlock',
		'color'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
AbstractWidgetPresenter class >> adapterName [

	^ self subclassResponsibility
]

{ #category : #specs }
AbstractWidgetPresenter class >> defaultSpec [

	^ { self adapterName. #adapt:. #model }
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> acceptDropBlock [

	^ acceptDropBlock value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> acceptDropBlock: aBlock [

	acceptDropBlock value: aBlock
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self acceptDropBlock cull: draggedMorph model model cull: event cull: source cull: source model 

]

{ #category : #api }
AbstractWidgetPresenter >> borderColor [
	"<api: #inspect>"
	"Return the border color"
	
	^ borderColor value
]

{ #category : #api }
AbstractWidgetPresenter >> borderColor: aColor [
	"<api: #color getter:#borderColor registration: #whenBorderColorChanged:>"
	"Set the border width"
	
	borderColor value: aColor 
]

{ #category : #api }
AbstractWidgetPresenter >> borderWidth [
	"<api: #inspect>"
	"Return the border width"
	
	^ borderWidth value
]

{ #category : #api }
AbstractWidgetPresenter >> borderWidth: anInteger [
	"<api: #integer min:0 max: 5 getter:#borderWidth registration: #whenBorderWidthChanged:>"
	"Set the border width"
	
	borderWidth value: anInteger
]

{ #category : #api }
AbstractWidgetPresenter >> color [

	^ color value
]

{ #category : #api }
AbstractWidgetPresenter >> color: aColor [

	^ color value: aColor
]

{ #category : #initialization }
AbstractWidgetPresenter >> defaultColor [
	^ Smalltalk ui theme backgroundColor
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> defaultTransferFor: passenger from: source [

	^ TransferPresenter new
		transfer: passenger;
		from: source;
		yourself
]

{ #category : #api }
AbstractWidgetPresenter >> disable [
	"<api: #do>"
	"Disable the label"
	
	self enabled: false
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dragEnabled [

	^ dragEnabled value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dragEnabled: anObject [

	dragEnabled value: anObject
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dragTransformationBlock [

	^ dragTransformationBlock value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dragTransformationBlock: aBlock [

	dragTransformationBlock value: aBlock
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dropEnabled [

	^ dropEnabled value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> dropEnabled: aBoolean [

	dropEnabled value: aBoolean
]

{ #category : #api }
AbstractWidgetPresenter >> enable [
	"<api: #do>"
	"Enable the label"
	
	self enabled: true
]

{ #category : #api }
AbstractWidgetPresenter >> enabled [
	"<api: #inspect>"
	^ enabledHolder value
]

{ #category : #api }
AbstractWidgetPresenter >> enabled: aBoolean [
	"<api: #boolean getter: #enabled registration: #whenEnabledChanged:>"
	"Set if the button is enabled (clickable)"
	
	enabledHolder value: aBoolean
]

{ #category : #api }
AbstractWidgetPresenter >> enabledHolder [
	
	^ enabledHolder
]

{ #category : #'api-focus' }
AbstractWidgetPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"
	
	^ { Character arrowRight asKeyCombination. Character tab asKeyCombination}
]

{ #category : #'api-focus' }
AbstractWidgetPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the next widget"
	
	^ { Character arrowLeft asKeyCombination. Character tab shift asKeyCombination}
]

{ #category : #api }
AbstractWidgetPresenter >> help [
	"<api: #inspect>"
	"Return the ballon text"
	
	^ helpHolder value
]

{ #category : #api }
AbstractWidgetPresenter >> help: aString [
	"<api: #string getter:#help registration: #whenHelpChanged:>"
	"Set the help string"
	
	helpHolder value: aString
]

{ #category : #initialization }
AbstractWidgetPresenter >> initialize [

	super initialize.

	helpHolder := nil asValueHolder.
	borderWidth := 0 asValueHolder.
	borderColor := Color transparent asValueHolder.
	enabledHolder := true asValueHolder.
	dragEnabled := false asValueHolder.
	dropEnabled := false asValueHolder.
	dragTransformationBlock := [ :item :source | item ] asValueHolder.
	wantDropBlock := [ :draggedItem :event :source | self dropEnabled ] asValueHolder.
	acceptDropBlock := [ :draggedItem :event :source | ] asValueHolder.
	transferBlock := [:passenger :source | self defaultTransferFor: passenger from: source ] asValueHolder.
	color := self defaultColor asValueHolder.
	
	helpHolder whenChangedDo: [:string | 
		self widget ifNotNil: [:w | w update: #setBalloonText: with: { string } ] ].
	
	borderWidth whenChangedDo: [:int | 
		self widget ifNotNil: [:w | w update: #borderWidth: with: { int } ] ].
	
	borderColor whenChangedDo: [:newColor | 
		self widget ifNotNil: [:w | w update: #borderColor: with: { newColor } ] ].
	
	color whenChangedDo: [:newColor | 
		self widget ifNotNil: [:w | w update: #color: with: { newColor } ] ].
	
	enabledHolder whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #enabled: with: { b } ] ].
	
	dragEnabled whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #dragEnabled: with: { b } ] ].
	
	dropEnabled whenChangedDo: [:b |
		self widget ifNotNil: [:w | w update: #dropEnabled: with: { b } ] ].
]

{ #category : #initialization }
AbstractWidgetPresenter >> initializeWidgets [ 
	"ignore this method since there is no composition in basic widgets"
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> transferBlock [

	^ transferBlock value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> transferBlock: aBlock [

	transferBlock value: aBlock
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> transferFor: passenger from: source [

	^ transferBlock value value: passenger value: source
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> wantDropBlock [

	^ wantDropBlock value
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> wantDropBlock: aBlock [

	wantDropBlock value: aBlock
]

{ #category : #'drag and drop' }
AbstractWidgetPresenter >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [
	draggedMorph isTransferable ifFalse: [ ^false ].
	
	^wantDropBlock value cull: draggedMorph passenger cull: anEvent cull:source
]

{ #category : #'api-events' }
AbstractWidgetPresenter >> whenBorderColorChanged: aBlock [
	"Set a block to be performed when the brder width changed"
	
	borderColor whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractWidgetPresenter >> whenBorderWidthChanged: aBlock [
	"Set a block to be performed when the brder width changed"
	
	borderWidth whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractWidgetPresenter >> whenEnabledChanged: aBlock [
	"Set a block to performed when the widget is enabled or disabled"
	
	enabledHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
AbstractWidgetPresenter >> whenHelpChanged: aBlock [
	"Set a block to be performed when the help changed"
	
	helpHolder whenChangedDo: aBlock
]
