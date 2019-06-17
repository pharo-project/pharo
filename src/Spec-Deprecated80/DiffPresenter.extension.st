Extension { #name : #DiffPresenter }

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> aboutToStyle: aBoolean [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'
]

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> behavior [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'.
	^ self contextClass
]

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> behavior: aClass [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'.
	self contextClass: aClass
]

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> doItReceiver: aReceiver [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'
]

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> getText [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'.
	^ self rightText
]

{ #category : #'*Spec-Deprecated80' }
DiffPresenter >> text: aPairOfString [
	self
		deprecated:
			'This method is an horrible hack because the VersionBrowser was using Code or Diff presenter but was only using the API of the Code presenter... This method and other methods from the CodePresenter API will be removed.'.
	(aPairOfString isText or: [ aPairOfString isString ])
		ifTrue: [ self leftText: ''.
			self rightText: aPairOfString ]
		ifFalse: [ self leftText: aPairOfString first.
			self rightText: aPairOfString second ]
]
