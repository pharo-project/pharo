ZnRequestReadEvent is signalled when an HTTP server reads a request in duration milliseconds.

Note that the duration can be misleading: when servicing multiple requests over a kept alive connection (the default for HTTP 1.1), the read time includes any wait time (as long as it is below the timeout). The first request read should not contain wait time.