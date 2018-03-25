bootstrapping
createImage

	| builder |

	builder := PBImageBuilderSpur50 forArchitecture: architecture.
	builder definitionFetcher: versionFetcher.
	builder buildNumber: buildNumber.
	builder imageFileReference: imageReference.
	builder systemDefinition: ringEnvironment.
		
	builder bootstrap