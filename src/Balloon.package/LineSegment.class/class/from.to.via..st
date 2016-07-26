geometry
from: startPoint to: endPoint via: via
	(startPoint = via or: [ endPoint = via ]) ifTrue: [ ^self new from: startPoint to: endPoint ].
	^Bezier2Segment from: startPoint to: endPoint via: via