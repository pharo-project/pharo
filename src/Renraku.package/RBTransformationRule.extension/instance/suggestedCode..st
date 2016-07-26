*renraku
suggestedCode: aCritic

	^ (self changes detect: [ :ch | 
			(ch  isMeta = aCritic  methodClass isMeta) &
			(ch changeClassName = aCritic criticTheNonMetaclassClass name) & 
			(ch selector = aCritic selector)]) source.
		