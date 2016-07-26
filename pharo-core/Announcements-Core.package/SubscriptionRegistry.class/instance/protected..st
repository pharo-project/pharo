private
protected: aBlock
	^ monitor critical: [ aBlock value ]