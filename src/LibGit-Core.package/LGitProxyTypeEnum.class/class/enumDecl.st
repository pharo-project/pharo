enum declaration
enumDecl
	^ #(
	"/**
	 * Do not attempt to connect through a proxy
	 *
	 * If built against libcurl, it itself may attempt to connect
	 * to a proxy if the environment variables specify it.
	 */"
	GIT_PROXY_NONE 0
	"/**
	 * Try to auto-detect the proxy from the git configuration.
	 */"
	GIT_PROXY_AUTO 1
	"/**
	 * Connect via the URL given in the options
	 */"
	GIT_PROXY_SPECIFIED 2
	)