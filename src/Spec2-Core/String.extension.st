Extension { #name : #String }

{ #category : #'*Spec2-Core' }
String >> asPresenter [ 

	^ SpLabelPresenter new 
		label: self;
		yourself
]
