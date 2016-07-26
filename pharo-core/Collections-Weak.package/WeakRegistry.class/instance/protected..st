private
protected: aBlock
	"Execute aBlock protected by the accessLock"

	^ sema
		critical: aBlock
		ifError: [ :msg :rcvr |
		rcvr error: msg ] 