private
helpText
	^ '# Pharo Smalltalk REPL. POST expressions to evaluate\# Here is one way (type ctrl-d to end input)\curl -X POST -H''Content-Type:text/plain'' --data-binary @- http://localhost:{1}/repl\' 
			withCRs 
				replaceAll: Character cr with: Character lf; 
				yourself