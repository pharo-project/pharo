preparation
generateHermesFiles
	| oldPackage hePackage writer |
	self hermesPackageNames
		do: [ :packageName | 
			oldPackage := ringEnvironment ask packageNamed: packageName.
			hePackage := HERing2ToHermesBuilder new visitPackage: oldPackage.
			writer := HEBinaryReaderWriter new
				stream: (File openForWriteFileNamed: (self bootstrapCacheDirectory / packageName , 'hermes') fullName);
				yourself.
			hePackage writeInto: writer.
			writer flush.
			writer close]