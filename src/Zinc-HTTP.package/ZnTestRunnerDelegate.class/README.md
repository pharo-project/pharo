I am ZnTestRunnerDelegate, a web service interface to run unit tests.

	ZnTestRunnerDelegate startInServerOn: 1701.
	
	ZnEasy get: 'http://localhost:1701/sunit/ZnUtilsTests'.
	ZnEasy get: 'http://localhost:1701/sunit/ZnUtilsTests/testBase64'.

Web Service API:

	GET /sunit/MyTestCase
	GET /sunit/MyTestCase/testOne

Part of Zinc HTTP Components