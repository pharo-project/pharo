"
SimpleGridExample new open
"
Class {
	#name : #SimpleGridExample,
	#superclass : #MorphTreeModel,
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #examples }
SimpleGridExample class >> example [

	SimpleGridExample new open
]

{ #category : #'instance creation' }
SimpleGridExample >> open [
	(self treeMorph buildContents;
		embeddedInMorphicWindowLabeled: 'Simple grid example') openInWorld
]

{ #category : #accessing }
SimpleGridExample >> rootItems [
	^ rootItems ifNil: [rootItems := (1 to: 1000)
		collect: [:i | PersonDataExample
				firstName: 'Person' , i asString
				secondName: 'Person' , i asString , ' second name'
				age: i
				married: false] ]
]

{ #category : #accessing }
SimpleGridExample >> rootNodeClassFromItem: anItem [
	^ SimpleGridNodeExample
]

{ #category : #'user interface' }
SimpleGridExample >> treeMorph [
	| treeMorph oddColor evenColor |
	treeMorph := (self treeMorphClass on: self)
		beCheckList;
		beMultiple;
		columns: {MorphTreeColumn new startWidth: 100;
						rowMorphGetSelector: #firstNameMorph;
						headerButtonLabel: 'First name' font:  nil.
					MorphTreeColumn new startWidth: 150;
						rowMorphGetSelector: #secondNameMorph;
						headerButtonLabel: 'Second name' font:  nil.
					MorphTreeColumn new startWidth: 50;
						rowMorphGetSelector: #ageMorph;
						headerButtonLabel: 'Age' font:  nil.
					MorphTreeColumn new startWidth: 50;
						rowMorphGetSelector: #marriedMorph;
					 	headerButtonLabel: 'Married' font: nil};
		columnInset: 3;
		rowInset: 1;
		preferedPaneColor: Color white;
		rowColorForEven:  Color veryLightGray muchLighter odd: Color white .
	^ treeMorph 
]
