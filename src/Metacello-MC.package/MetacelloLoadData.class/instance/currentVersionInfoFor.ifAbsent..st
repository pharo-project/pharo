versionInfo
currentVersionInfoFor: packageSpec ifAbsent: aBlock

	^self versionInfoMap 
		at: packageSpec file 
		ifPresent: [ :v | v first ]
		ifAbsent: [ 
			self  packageNameMap 
				at: packageSpec name
				ifPresent: [ :v | v first ]
				ifAbsent: aBlock ]