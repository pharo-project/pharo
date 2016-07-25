instance creation
package: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection baseVersion: aVersion
	^ self 
		package: aPackage
		info: aVersionInfo
		dependencies: aCollection
		baseInfo: aVersion info
		patch: (aSnapshot patchRelativeToBase: aVersion snapshot)