private
writeResponseBad: exception on: stream timing: timing
	"When we failed to parse a request, write a bad request response before closing."

	| response |
	response := ZnResponse new
		statusLine: ZnStatusLine badRequest;
		headers: ZnHeaders defaultResponseHeaders;
		entity: (ZnEntity textCRLF: 'Bad Request ' , exception printString);
		yourself.
	response setConnectionClose.
	self writeResponseSafely: response on: stream timing: timing