private
isNativeSSLPluginPresent
	"Return whether the SSL VM plugin can be instanciated and intialized."
	
	^ [ ZdcPluginSSLSession new destroy. true ]
			on: ZdcPluginMissing
			do: [ false ]