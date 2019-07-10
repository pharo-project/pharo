"
I am the adapter used to bridget a LabelPresenter and a LabelMorph
"
Class {
	#name : #MorphicLabelAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'emphasisOptions'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #initialization }
MorphicLabelAdapter >> applyStyle: aMorph [

	super applyStyle: aMorph.
	aMorph fitContents
]

{ #category : #factory }
MorphicLabelAdapter >> buildWidget [
	| label |
	label := LabelMorph new model: self.
	label
		getEnabledSelector: #enabled;
		getTextSelector: #getText;
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setBalloonText: self help;
		color: self model color;
		emphasis: (self emphasisCodeFor: self model emphasis);
		setProperty: #minWidth toValue: 3. "3 is the default value of StringMorph. We do not want the default value of LabelMorph that is the min width of the content..."
		
	^ label
]

{ #category : #'spec protocol' }
MorphicLabelAdapter >> emphasis: anEmphasis [

	self widgetDo: [ :w | w emphasis: (self emphasisCodeFor: anEmphasis) ]
]

{ #category : #'spec protocol' }
MorphicLabelAdapter >> emphasisCodeFor: anEmphasis [
	^ anEmphasis
		inject: 0
		into: [ :sum :next | sum + (self emphasisOptions at: next) ]
]

{ #category : #'spec protocol' }
MorphicLabelAdapter >> emphasisOptions [
	^ emphasisOptions
		ifNil:
			[ 
			emphasisOptions := {(#bold -> 1).
			(#italic -> 2).
			(#underline -> 4).
			(#narrow -> 8).
			(#struck -> 16)} asDictionary ]
]

{ #category : #'model access' }
MorphicLabelAdapter >> getText [

	^ self model label
]

{ #category : #accessing }
MorphicLabelAdapter >> label [
	
	^ widget contents
]
