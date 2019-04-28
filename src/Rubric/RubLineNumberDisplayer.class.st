"
I am the displayer of line numbers in the text area
"
Class {
	#name : #RubLineNumberDisplayer,
	#superclass : #RubScrolledTextSideRuler,
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubLineNumberDisplayer class >> key [
	^ #lineNumbers
]

{ #category : #'menu declaration' }
RubLineNumberDisplayer class >> menuOn: aBuilder [
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubLineNumberMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<RubLineNumberMenu>
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		iconName: #smallFindIcon

]

{ #category : #accessing }
RubLineNumberDisplayer >> backgroundColor [
	^ self paragraphProvider lineNumbersBackgroundColor 
]

{ #category : #geometry }
RubLineNumberDisplayer >> computedWidthFrom: aRectangle [
	^ (((self fontToUse widthOfString: self paragraph numberOfPhysicalLines asString) + self horizontalGapBefore
		+ self horizontalGapAfter + self verticalSeparatorWidth) max: self minimumWidth) truncated
]

{ #category : #drawing }
RubLineNumberDisplayer >> drawOn: aCanvas [
	| prev right font lines visibleRectangle tcolor left |
	super drawOn: aCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	right := self bounds right.
	left := self bounds left.
	font := self fontToUse.
	prev := nil.
	lines := self lines.
	tcolor := self textColor.
	aCanvas
		clipBy: self bounds
		during: [ :clippedCanvas | 
			| backBnd |
			clippedCanvas fillRectangle: self verticalSeparatorBounds color: self verticalSeparatorColor.
			visibleRectangle := clippedCanvas clipRect.
			(self lineIndexForPoint: visibleRectangle topLeft) to: (self lineIndexForPoint: visibleRectangle bottomRight) do: [ :i | 
				| line |
				line := lines at: i.
				(self selectionStart == self selectionStop and: [self selectionStop textLine lineNumber = line lineNumber])
					ifTrue: [ 
						backBnd := self bounds.
						backBnd := (backBnd left) @ line top corner: self verticalSeparatorBounds left @ line bottom.
						backBnd := backBnd translateBy: 0 @ (self offset y negated + self bounds top).
						clippedCanvas fillRectangle: backBnd color: self selectionColorToUse ].
				prev = line lineNumber
					ifFalse: [ 
						| lineMark lineBnd ygap |
						ygap := ((line lineHeight - font height) // 2 - 1) rounded.
						lineMark := line lineNumber asString.
						self side = #left
							ifTrue: [ 
								| width |
								width := (self fontToUse widthOfString: lineMark) + self horizontalGapAfter.
								lineBnd := (right - width) @ (line bottom - font height - ygap) corner: (right + width) @ line bottom ]
							ifFalse: [ lineBnd := (left + self horizontalGapBefore) @ (line bottom - font height - ygap) corner: right @ line bottom ].
						lineBnd := lineBnd translateBy: 0 @ (self offset y negated + self bounds top).
						clippedCanvas
							drawString: lineMark
							in: lineBnd
							font: font
							color: tcolor.
						prev := line lineNumber ] ] ]
]

{ #category : #drawing }
RubLineNumberDisplayer >> drawOnAthensCanvas: aCanvas [
	| prev right font lines visibleRectangle tcolor left |
	super drawOnAthensCanvas: aCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	right := self bounds right.
	left := self bounds left.
	font := self fontToUse.
	prev := nil.
	lines := self lines.
	tcolor := self textColor.
	aCanvas
		clipBy: self bounds
		during: [ 
			| backBnd |
			aCanvas setShape: self verticalSeparatorBounds.
			aCanvas setPaint: self verticalSeparatorColor.
			aCanvas draw.
			visibleRectangle := aCanvas clipRect.
			(self lineIndexForPoint: visibleRectangle topLeft) to: (self lineIndexForPoint: visibleRectangle bottomRight) do: [ :i | 
				| line |
				line := lines at: i.
				(self selectionStart == self selectionStop and: [self selectionStop textLine lineNumber = line lineNumber])
					ifTrue: [ 
						backBnd := self bounds.
						backBnd := (backBnd left) @ line top corner: self verticalSeparatorBounds left @ line bottom.
						backBnd := backBnd translateBy: 0 @ (self offset y negated + self bounds top).
						aCanvas setShape: backBnd.
						aCanvas setPaint: self selectionColorToUse.
						aCanvas draw ].
				prev = line lineNumber
					ifFalse: [ 
						| lineMark lineBnd ygap |
						ygap := ((line lineHeight - font height) // 2 - 1) rounded.
						lineMark := line lineNumber asString.
						self side = #left
							ifTrue: [ 
								| width |
								width := (self fontToUse widthOfString: lineMark) + self horizontalGapAfter.
								lineBnd := (right - width) @ (line bottom - font height - ygap) corner: (right + width) @ line bottom ]
							ifFalse: [ lineBnd := (left + self horizontalGapBefore) @ (line bottom - font height - ygap) corner: right @ line bottom ].
						lineBnd := lineBnd translateBy: 0 @ (self offset y negated + self bounds top).
						aCanvas morphicDrawString: lineMark in: lineBnd font: font color: tcolor.
						prev := line lineNumber ] ] ]
]

{ #category : #accessing }
RubLineNumberDisplayer >> fontToUse [
	^ RubAbstractTextArea lineNumbersFont 
]

{ #category : #menus }
RubLineNumberDisplayer >> getMenu: shiftState [
	"Answer the menu to be presented when the yellow button is pressed while the shift key is down"

	^ nil "self menu"
]

{ #category : #'event handling' }
RubLineNumberDisplayer >> handlesMouseDown: evt [
	^ true
]

{ #category : #geometry }
RubLineNumberDisplayer >> horizontalGapAfter [
	^ 3
]

{ #category : #geometry }
RubLineNumberDisplayer >> horizontalGapBefore [
	^ 3
]

{ #category : #accessing }
RubLineNumberDisplayer >> level [
	^ 2
]

{ #category : #menus }
RubLineNumberDisplayer >> menu [
	"Answer the menu to be presented when the yellow button is pressed while the shift key is down"

	^ (PragmaMenuBuilder pragmaKeyword: self menuKeyword  model: self) menu
]

{ #category : #menus }
RubLineNumberDisplayer >> menuKeyword [ 
	^ #RubLineNumberMenu
	
]

{ #category : #geometry }
RubLineNumberDisplayer >> minimumWidth [
	^ (self fontToUse widthOfString: 'MM') + self horizontalGapAfter + self horizontalGapBefore + self  verticalSeparatorWidth 

]

{ #category : #'event handling' }
RubLineNumberDisplayer >> mouseDown: anEvent [
	| lineIndex line |
	(anEvent yellowButtonPressed and: [ anEvent commandKeyPressed not ]) "First check for option (menu) click"
		ifTrue: [ ^ (self yellowButtonActivity: anEvent shiftPressed) ].

	lineIndex := self lineIndexForPoint: anEvent position.
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft.
	self textArea mouseDown: anEvent
]

{ #category : #'event handling' }
RubLineNumberDisplayer >> mouseMove: anEvent [
	| lineIndex line |
	lineIndex := self lineIndexForPoint: anEvent position.
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft.
	self textArea mouseMove: anEvent

]

{ #category : #'event handling' }
RubLineNumberDisplayer >> mouseUp: anEvent [
	| lineIndex line |
	lineIndex := self lineIndexForPoint: anEvent position.
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft.
	self textArea mouseUp: anEvent.

]

{ #category : #accessing }
RubLineNumberDisplayer >> selectionColorToUse [
	^ self primarySelectionColor
]

{ #category : #accessing }
RubLineNumberDisplayer >> textColor [
	^ self textArea lineNumbersTextColor 

]

{ #category : #accessing }
RubLineNumberDisplayer >> verticalSeparatorBounds [
	| bnds  |
	bnds := self bounds.
	^ self side = #left
		ifTrue: [ (bnds topRight -  (self verticalSeparatorWidth @ 0)) corner: bnds bottomRight ]
		ifFalse: [ (bnds topLeft corner: bnds bottomLeft + (self verticalSeparatorWidth @ 0)) ]
]

{ #category : #accessing }
RubLineNumberDisplayer >> verticalSeparatorColor [
	^ (self textArea backgroundColor contrastingBlackAndWhiteColor ) alpha: 0.3
]

{ #category : #accessing }
RubLineNumberDisplayer >> verticalSeparatorWidth [
	^ 1
]

{ #category : #'event handling' }
RubLineNumberDisplayer >> yellowButtonActivity: shiftKeyState [ 
	"Invoke the text-editing menu.
	Check if required first!"
	(self getMenu: shiftKeyState)
		ifNotNil: [ :menu|
			menu invokeModal. 
			self changed.
			^ true].
	^ true
]
