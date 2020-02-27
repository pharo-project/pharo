"
I am the adapter used to bridge a TextPresenter and a PluggableTextMorph
"
Class {
	#name : #MorphicTextAdapter,
	#superclass : #AbstractMorphicAdapter,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'spec protocol' }
MorphicTextAdapter >> accept [
	"We use RubScrolledTextMorph>>#acceptContents but we keep accept as method name for compatibility"
	self widgetDo: [ :w | w acceptContents ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> accept: aText notifying: aNotifyier [

	self model accept: aText notifying: aNotifyier
]

{ #category : #accessing }
MorphicTextAdapter >> acceptAllowed [
	^self widget editingMode acceptAllowed
]

{ #category : #accessing }
MorphicTextAdapter >> announcer [
	^ self model announcer

	
]

{ #category : #'widget API' }
MorphicTextAdapter >> askBeforeDiscardingEdits [

	^ self model askBeforeDiscardingEdits
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> askBeforeDiscardingEdits: aBoolean [
	self widget askBeforeDiscardingEdits: aBoolean
]

{ #category : #'widget API' }
MorphicTextAdapter >> autoAccept [

	^ self model autoAccept
]

{ #category : #'widget API' }
MorphicTextAdapter >> autoAccept: aBoolean [

	^ self model autoAccept: aBoolean
]

{ #category : #'widget API' }
MorphicTextAdapter >> behavior [

	^ self model behavior
]

{ #category : #factory }
MorphicTextAdapter >> buildWidget [
	| newWidget |
	newWidget := RubPluggableTextMorph new
		on: self
			text: #getText
			accept: #accept:notifying:
			readSelection: #readSelection
			menu: #codePaneMenu:shifted:
			setSelection: #setSelection:;
		beWrapped;
		enabled: self enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		registerScrollChanges: #scrollValueChanged:;
		yourself.
	self setEditingModeFor: newWidget withBehavior: self behavior.
	^ newWidget
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> canChangeText [
	^ self widget textArea canChangeText
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> classOrMetaClass: aClass [
	self setEditingModeFor: self widget withBehavior: aClass
]

{ #category : #'undo-redo' }
MorphicTextAdapter >> clearUndoManager [
	self widget clearUndoManager
]

{ #category : #'widget API' }
MorphicTextAdapter >> clearUserEditFlag [

	self model text: self getText.
	self changed: #clearUserEdits
]

{ #category : #'widget API' }
MorphicTextAdapter >> codePaneMenu: aMenu shifted: shifted [ 

	^ self model codePaneMenu: aMenu shifted: shifted 
]

{ #category : #'widget API' }
MorphicTextAdapter >> doItContext [

	^ self model doItContext
]

{ #category : #'widget API' }
MorphicTextAdapter >> doItReceiver [

	^ self model doItReceiver
]

{ #category : #accessing }
MorphicTextAdapter >> editingMode [ 

	^ self widget editingMode
]

{ #category : #'widget API' }
MorphicTextAdapter >> font: aFont [
	self widgetDo: [ :w | w font: aFont ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> getText [

	^ self model text
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> hasEditingConflicts: aBoolean [
 
	self widgetDo: [ :w | w hasEditingConflicts: aBoolean ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> hasUnacceptedEdits: aBoolean [

	self model hasUnacceptedEdits: aBoolean
]

{ #category : #'protocol-shout' }
MorphicTextAdapter >> isAboutToStyle [

	^ self model isAboutToStyle
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> isForSmalltalkCode [
	^ self model isForSmalltalkCode
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> notify: errorMessage at: position in: sourceCode [

	self widgetDo: [ :w | w notify: errorMessage at: position in: sourceCode ]
]

{ #category : #'private-shout' }
MorphicTextAdapter >> okToStyle [

	^ true
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> pendingText [

	^ self widgetDo: [ :w | w text ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> pendingText: aText [

	self widgetDo: [ :w | 
		w setText: aText.
		w hasUnacceptedEdits: true ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> readSelection [

	^ self model readSelection
]

{ #category : #NOCompletion }
MorphicTextAdapter >> receiverClass [

	^ self behavior
]

{ #category : #'widget API' }
MorphicTextAdapter >> scrollValueChanged: aValue [

	self model scrollValue: aValue step
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectAll [
	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectFrom: nodeStart to: nodeStop [
	self widget selectFrom: nodeStart to: nodeStop
]

{ #category : #'widget API' }
MorphicTextAdapter >> selectedBehavior [

	^ self model selectedBehavior
]

{ #category : #NOCompletion }
MorphicTextAdapter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectionInterval [
	^ self widget selectionInterval 
]

{ #category : #private }
MorphicTextAdapter >> setEditingModeFor: textArea withBehavior: behavior [
	behavior
		ifNil:
			[
			self isForSmalltalkCode
				ifTrue: [ textArea beForSmalltalkScripting ]
				ifFalse: [ textArea beForPlainText ] ]
		ifNotNil: [ :class | 
			textArea
				beForSmalltalkCodeInClass: class ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> setHasUnacceptedEdits: aBoolean [

	self widgetDo: [ :w | w basicHasUnacceptedEdits: aBoolean ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> setScrollValue: aValue [
	self
		widgetDo: [ :w | 
			w scrollPane
				hScrollbarValue: aValue x;
				vScrollbarValue: aValue y;
				updateScrollbars ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> setSelection: interval [

	self model setSelectionInterval: interval
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> setSelectionFromModel: aSelection [

	self widget ifNotNil: [:w | w setSelection: aSelection ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> shoutAboutToStyle: aMorph [
	
	^ self model isAboutToStyle
]

{ #category : #'widget API' }
MorphicTextAdapter >> wantsVisualFeedback [

	^ self model wantsVisualFeedback
]
