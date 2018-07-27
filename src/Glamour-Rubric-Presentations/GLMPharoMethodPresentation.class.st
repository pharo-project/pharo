"
I provide support for displaying a Smalltalk method.

self pharoMethodPresentationExample



"
Class {
	#name : #GLMPharoMethodPresentation,
	#superclass : #GLMRubricSmalltalkCodePresentation,
	#category : #'Glamour-Rubric-Presentations'
}

{ #category : #examples }
GLMPharoMethodPresentation class >> pharoMethodPresentationExample [
	"
	self pharoMethodPresentationExample
	"
	
	| browser |
	browser := GLMTabulator new.
	browser
		row: #r1;
		row: #r2.
	browser transmit
		to: #r1;
		andShow: [ :aComposite | aComposite pharoMethod display: [ :method | method sourceCode ] ].
	browser transmit
		to: #r2;
		andShow: [ :aComposite | 
					aComposite pharoMethod
						display: [ :method | method sourceCode ];
						smalltalkClass: [ :method | method methodClass ] ].
	browser openOn: CompiledMethod class >> #gtExampleSimple
]

{ #category : #actions }
GLMPharoMethodPresentation >> formatSourceCode [
	self announce: GLMFormatCode.
]

{ #category : #rendering }
GLMPharoMethodPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderPharoMethodPresentation: self
]
