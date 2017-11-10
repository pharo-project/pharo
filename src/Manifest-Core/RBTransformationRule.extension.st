Extension { #name : #RBTransformationRule }

{ #category : #'*Manifest-Core' }
RBTransformationRule >> transform: aCritic [
	| changeCode |

	changeCode := (self changes detect: [ :ch | 
			(ch  isMeta = aCritic  methodClass isMeta) &
			(ch changeClassName = aCritic criticTheNonMetaclassClass name) & 
			(ch selector = aCritic selector)]) source.
		
	aCritic methodClass compile: changeCode
]
