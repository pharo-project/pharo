Extension { #name : #Form }

{ #category : #'*Spec2-Core' }
Form >> asPresenter [

	^ SpImagePresenter new 
		image: self;
		yourself
]
