"
I am the adapter used to bridget a LabelPresenter and a LabelMorph
"
Class {
	#name : #SpMorphicLabelAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'emphasisOptions'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicLabelAdapter >> applyStyle: aMorph [

	super applyStyle: aMorph.
	aMorph fitContents
]

{ #category : #factory }
SpMorphicLabelAdapter >> buildWidget [
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
SpMorphicLabelAdapter >> emphasis: anEmphasis [

	self widgetDo: [ :w | w emphasis: (self emphasisCodeFor: anEmphasis) ]
]

{ #category : #'spec protocol' }
SpMorphicLabelAdapter >> emphasisCodeFor: anEmphasis [
	^ anEmphasis
		inject: 0
		into: [ :sum :next | sum + (self emphasisOptions at: next) ]
]

{ #category : #'spec protocol' }
SpMorphicLabelAdapter >> emphasisOptions [
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
SpMorphicLabelAdapter >> getText [

	^ self model label
]

{ #category : #accessing }
SpMorphicLabelAdapter >> label [
	
	^ widget contents
]
