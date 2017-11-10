Extension { #name : #DiffPresenter }

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> aboutToStyle: aBoolean [
]

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> behavior [
	^ self contextClass
]

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> behavior: aClass [

	self contextClass: aClass
]

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> doItReceiver: aReceiver [
]

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> getText [

	^ self rightText
]

{ #category : #'*Spec-Tools-VersionBrowser' }
DiffPresenter >> text: aPairOfString [

	(aPairOfString isText or: [aPairOfString isString])
		ifTrue: [
			self leftText: ''.
			self rightText: aPairOfString
			 ]
		ifFalse: [
			self leftText: aPairOfString first.
			self rightText: aPairOfString second ]
]
