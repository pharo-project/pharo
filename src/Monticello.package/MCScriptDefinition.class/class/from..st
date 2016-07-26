instance-creation
from: aPackageInfo
	^ self script: (aPackageInfo perform: self scriptSelector) contents asString packageName: aPackageInfo name