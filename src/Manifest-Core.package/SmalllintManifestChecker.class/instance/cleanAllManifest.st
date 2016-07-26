manifest
cleanAllManifest
	| manifests |
	manifests := environment packages collect: [ :cl | self builderManifestClass of: cl ] as: Set.
	manifests do: [ :each | each cleanUp ]