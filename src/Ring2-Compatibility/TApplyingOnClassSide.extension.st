Extension { #name : #TApplyingOnClassSide }

{ #category : #'*Ring2-Compatibility' }
TApplyingOnClassSide >> asRing2Definition [

	^ self theNonMetaClass asRing2Definition theMetaClass
]
