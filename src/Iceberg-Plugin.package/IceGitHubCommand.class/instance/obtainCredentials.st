private
obtainCredentials
	| newCredentials provider |
	
	provider := IceCredentialsProvider default.
	newCredentials := IcePlaintextCredentials new.
	IceCredentialsProvider default providePlaintextCredentialsTo: newCredentials.
	^ newCredentials

