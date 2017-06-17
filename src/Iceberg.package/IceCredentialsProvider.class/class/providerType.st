accessing
providerType
	^ ProviderType ifNil: [ ProviderType := IceCredentialsProvider ]