private
doesNativeSSLPluginSupportSNI
	"Return whether the SSL VM plugin supports SNI."
	
	^ [ ZdcPluginSSLSession new serverName; serverName: nil; destroy. true ]
			on: ZdcPluginMissing, PrimitiveFailed
			do: [ false ]