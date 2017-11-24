"
A GLMBandBrick is a simple Brick with height that shrinks to wrap subbricks.
Usually used within scroll panes
"
Class {
	#name : #GLMBandBrick,
	#superclass : #GLMBrick,
	#category : #'Glamour-Morphic-Brick-Widgets-Core'
}

{ #category : #examples }
GLMBandBrick class >> exampleOpen [
	
	self gtExample openInBrickWindowLabeled: 'Band example'
]

{ #category : #examples }
GLMBandBrick class >> gtExample [
	<gtExample>
	<label: 'Example with subbricks'>
	
	^ self new
		hSpaceFill;
		vSpaceFill;
		with: [ :b | |count|
			count := 10.
			1 to: count do: [ :index | 
				b addBrickBack: (
					GLMLabelBrick new
						text: (index asString, ' of ', count asString);
						textPosition: #center;
						color: Color random muchLighter;
						height: 40;
						hSpaceFill ) ] ]
]

{ #category : #initialization }
GLMBandBrick >> initialize [
	super initialize.
	
	self
		useVerticalLinearLayout;
		vShrinkWrap
]
