class initialization
initialize
	CommonHeaders := (
		#(
			'Content-Length' 'Content-Type' 'Date' 'Server' 'Connection' 'User-Agent' 'Host'
			'Accept' 'Accept-Language' 'Accept-Encoding'
			'Referer' 'Dnt'
			'Cookie' 'Set-Cookie' 'Authorization' 'WWW-Authenticate' 'Proxy-Authorization'
			'Content-Encoding' 'Transfer-Encoding' 'Location'
			'If-Modified-Since' 'Content-Disposition' 
			'X-Zinc-Remote-Address' 'X-Forwarded-Server' 'X-Forwarded-For' 'X-Forwarded-Host' ) 
			collect: [ :each | ZnUtils capitalizeString: each ]) asSet