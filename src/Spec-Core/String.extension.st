Extension { #name : #String }

{ #category : #'*Spec-Core' }
String >> asPresenter [ 

	^ LabelPresenter new 
		label: self;
		yourself
]
