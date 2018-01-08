Class {
	#name : #GTPlaygroundEditableTabLabel,
	#superclass : #GLMEditableLabelBrick,
	#category : #'GT-Playground'
}

{ #category : #initialization }
GTPlaygroundEditableTabLabel >> defaultPadding [

	^ 5
]

{ #category : #initialization }
GTPlaygroundEditableTabLabel >> initialize [
	super initialize.
	
	self
		hSpaceFill;
		with: [ :label | label when: #onAccepted do: [ self announce: #accepted event: label text ] ];
		cancelOnEscape;
		cancelOnFocusLost;
		with: [ :b | b margin: { b themer focusedBorderWidth . 0 } ];
		with: [ :b | b editBrick
			hShrinkWrap;
			rubricBackgroundColor: b themer rubricBackgroundColor;
			margins: (self defaultPadding@0) asMargin ];
		with: [ :b | b maxWidth: [ b approximateWidth + b owner hPadding] ];
		with: [ :b | b labelBrick margin: { 0 . self defaultPadding + 2. 0 . self defaultPadding } ];
		with: [ :b | b layout widthDependency: [ b owner ] ];
		with: [ :b | b editBrick layout widthDependency: [ b ] ];
		yourself 
]
