I am ZnEasy, a simplified HTTP client for occasional use.

My class side implements a number of convenience methods to do common HTTP client operations.

	ZnEasy get: 'http://zn.stfx.eu/zn/small.html'.

For most requests, I return a response object. 

For a couple of message, I return an image Form.

	ZnEasy getPng: 'http://www.pharo-project.org/images/pharo.png'.

For my implementation I use ZnClient, a full featured HTTP client.

Part of Zinc HTTP Components.