scripting
from: aPort transformed: aBlock
	^ (self addActiveOrigin: (self browser resolveOriginPort: aPort)) transformation: aBlock