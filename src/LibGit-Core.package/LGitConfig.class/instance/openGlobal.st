accessing
openGlobal
	| outHandle |

	outHandle := ExternalAddress null.
	self config_open_global: outHandle.
	^ (self class fromHandle: outHandle) autoRelease
	