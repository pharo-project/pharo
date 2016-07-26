actions
linearLoadPackageSpec: packageSpec gofer: gofer

	MetacelloPlatform current
		do:  [ | externalReference loadBlock answers fake |
			externalReference := self resolveRecordingPackageSpecReference: packageSpec gofer: gofer.
			loadBlock := [
					self preLoad: packageSpec.
					(MetacelloDirective 
						loadPackage: packageSpec 
						externalReference: externalReference 
						loader: self) addTo: self loadDirective.
					self postLoad: packageSpec ].
			(answers := packageSpec answers) notEmpty
				ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
				ifFalse: [ loadBlock value ].
			fake := packageSpec copy.
			fake name: fake file.
			self loadData 
				addVersion: fake
				versionInfo: fake
				resolvedReference: externalReference 
				packageSpec: packageSpec ]
		displaying: 'Recording ', packageSpec file