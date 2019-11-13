"
SpAbstractBasicWidget is an abstract class for basic widgets

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
	#name : #SpAbstractWidgetPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'#borderWidth => SpObservableSlot',
		'#borderColor => SpObservableSlot',
		'#dragEnabled => SpObservableSlot',
		'#dropEnabled => SpObservableSlot',
		'#dragTransformationBlock => SpObservableSlot',
		'#wantDropBlock => SpObservableSlot',
		'#acceptDropBlock => SpObservableSlot',
		'#transferBlock => SpObservableSlot',
		'#color => SpObservableSlot',
		'#help => SpObservableSlot',
		'#enabled => SpObservableSlot',
		'#styles'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpAbstractWidgetPresenter class >> adapterName [

	^ self subclassResponsibility
]

{ #category : #specs }
SpAbstractWidgetPresenter class >> defaultSpec [
	<spec: #default>
	^ SpAbstractWidgetLayout for: self adapterName
]

{ #category : #testing }
SpAbstractWidgetPresenter class >> isAbstract [
	^ self = SpAbstractWidgetPresenter
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> acceptDropBlock [
	^ acceptDropBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> acceptDropBlock: aBlock [
	acceptDropBlock := aBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self acceptDropBlock cull: draggedMorph model model cull: event cull: source cull: source model 

]

{ #category : #api }
SpAbstractWidgetPresenter >> addStyle: aStyle [

	styles := styles copyWith: aStyle
]

{ #category : #api }
SpAbstractWidgetPresenter >> borderColor [
	"Return the border color"

	^ borderColor
]

{ #category : #api }
SpAbstractWidgetPresenter >> borderColor: aColor [
	"Set the border width"

	borderColor := aColor
]

{ #category : #api }
SpAbstractWidgetPresenter >> borderWidth [
	"Return the border width"

	^ borderWidth
]

{ #category : #api }
SpAbstractWidgetPresenter >> borderWidth: anInteger [
	"Set the border width"

	borderWidth := anInteger
]

{ #category : #api }
SpAbstractWidgetPresenter >> color [
	^ color
]

{ #category : #api }
SpAbstractWidgetPresenter >> color: aColor [
	^ color := aColor
]

{ #category : #initialization }
SpAbstractWidgetPresenter >> defaultColor [
	^ self theme backgroundColor
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> defaultTransferFor: passenger from: source [

	^ SpTransferPresenter new
		transfer: passenger;
		from: source;
		yourself
]

{ #category : #api }
SpAbstractWidgetPresenter >> disable [
	"Disable the label"
	
	self enabled: false
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dragEnabled [
	^ dragEnabled
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dragEnabled: aBoolean [
	dragEnabled := aBoolean
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dragTransformationBlock [
	^ dragTransformationBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dragTransformationBlock: aBlock [
	dragTransformationBlock := aBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dropEnabled [
	^ dropEnabled
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> dropEnabled: aBoolean [
	dropEnabled := aBoolean
]

{ #category : #api }
SpAbstractWidgetPresenter >> enable [
	"Enable the label"
	
	self enabled: true
]

{ #category : #api }
SpAbstractWidgetPresenter >> enabled: aBooleanOrValuable [
	"Set if the button is enabled (clickable).
	This can either be a boolean or a block returning a boolean."

	enabled := aBooleanOrValuable
]

{ #category : #'api-focus' }
SpAbstractWidgetPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"
	
	^ { Character arrowRight asKeyCombination. Character tab asKeyCombination}
]

{ #category : #'api-focus' }
SpAbstractWidgetPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the next widget"
	
	^ { Character arrowLeft asKeyCombination. Character tab shift asKeyCombination}
]

{ #category : #api }
SpAbstractWidgetPresenter >> help [
	"Return the help string to display as help to the user."

	^ help
]

{ #category : #api }
SpAbstractWidgetPresenter >> help: aString [
	"Set a help string to display to the user if he let the mouse over the widget."

	help := aString
]

{ #category : #initialization }
SpAbstractWidgetPresenter >> initialize [
	super initialize.

	borderWidth := 0.
	borderColor := Color transparent.
	enabled := true.
	dragEnabled := false.
	dropEnabled := false.
	dragTransformationBlock := [ :item :source | item ].
	wantDropBlock := [ :draggedItem :event :source | self dropEnabled ].
	acceptDropBlock := [ :draggedItem :event :source |  ].
	transferBlock := [ :passenger :source | self defaultTransferFor: passenger from: source ].
	color := self defaultColor.
	self whenHelpChangedDo: [ :string | self withAdapterDo: [ :w | w update: #setBalloonText: with: {string} ] ].
	self whenBorderWidthChangedDo: [ :int | self withWidgetDo: [ :w | w update: #borderWidth: with: {int} ] ].
	self whenBorderColorChangedDo: [ :newColor | self withWidgetDo: [ :w | w update: #borderColor: with: {newColor} ] ].
	self property: #color whenChangedDo: [ :newColor | self withAdapterDo: [ :w | w update: #color: with: {newColor} ] ].
	self whenEnabledChangedDo: [ :b | self withAdapterDo: [ :w | w update: #enabled: with: {b} ] ].
	self property: #dragEnabled whenChangedDo: [ :b | self withWidgetDo: [ :w | w update: #dragEnabled: with: {b} ] ].
	self property: #dropEnabled whenChangedDo: [ :b | self withWidgetDo: [ :w | w update: #dropEnabled: with: {b} ] ].

	styles := #()
]

{ #category : #initialization }
SpAbstractWidgetPresenter >> initializePresenters [ 
	"ignore this method since there is no composition in basic widgets"
]

{ #category : #api }
SpAbstractWidgetPresenter >> isEnabled [
	^ enabled
]

{ #category : #api }
SpAbstractWidgetPresenter >> removeStyle: aStyle [

	(styles includes: aStyle) ifFalse: [ aStyle ].
	styles := styles copyWithout: aStyle 
]

{ #category : #api }
SpAbstractWidgetPresenter >> styles [ 

	^ styles
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> transferBlock [
	^ transferBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> transferBlock: aBlock [
	transferBlock := aBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> transferFor: passenger from: source [
	^ self transferBlock value: passenger value: source
]

{ #category : #'private-traversing' }
SpAbstractWidgetPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	self canTakeKeyboardFocus ifFalse: [ ^ self ].
	
	excludes add: self.
	aBlock value: self
]

{ #category : #'private-traversing' }
SpAbstractWidgetPresenter >> traversePresentersDo: aBlock excluding: excludes [
	"Terminate"

	excludes add: self.
	aBlock value: self
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> wantDropBlock [
	^ wantDropBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> wantDropBlock: aBlock [
	wantDropBlock := aBlock
]

{ #category : #'drag and drop' }
SpAbstractWidgetPresenter >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [
	draggedMorph isTransferable ifFalse: [ ^ false ].

	^ self wantDropBlock cull: draggedMorph passenger cull: anEvent cull: source
]

{ #category : #'api-events' }
SpAbstractWidgetPresenter >> whenBorderColorChangedDo: aBlock [
	"Set a block to be performed when the brder width changed"

	self property: #borderColor whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractWidgetPresenter >> whenBorderWidthChangedDo: aBlock [
	"Set a block to be performed when the brder width changed"

	self property: #borderWidth whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractWidgetPresenter >> whenEnabledChangedDo: aBlock [
	"Set a block to performed when the widget is enabled or disabled"

	self property: #enabled whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractWidgetPresenter >> whenHelpChangedDo: aBlock [
	"Set a block to be performed when the help changed"

	self property: #help whenChangedDo: aBlock
]
