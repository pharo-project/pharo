testing
isBleedingEdge
	^  (self project hasVersion: #bleedingEdge) and: [
		(self project version: #bleedingEdge) = self version ]