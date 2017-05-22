field definition
fieldsDesc
	^ #(
	uint version;

	"/**
	 * The type of proxy to use, by URL, auto-detect.
	 */"
	LGitProxyTypeEnum type;

	"/**
	 * The URL of the proxy.
	 */"
	char *url;

	"/**
	 * This will be called if the remote host requires
	 * authentication in order to connect to it.
	 *
	 * Returning GIT_PASSTHROUGH will make libgit2 behave as
	 * though this field isn't set.
	 */"
	LGitCredAcquireCallback credentials;

	"/**
	 * If cert verification fails, this will be called to let the
	 * user make the final decision of whether to allow the
	 * connection to proceed. Returns 1 to allow the connection, 0
	 * to disallow it or a negative value to indicate an error.
	 */"
   LGitTransportCertificateCheckCallback certificate_check;

	"/**
	 * Payload to be provided to the credentials and certificate
	 * check callbacks.
	 */"
	void *payload;
	)