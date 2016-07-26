I am ZnImageExampleDelegate.
I am a implementation of the web app in 'Building and deploying your first web app in Pharo'.

I serve an image that web clients can change by uploading a new one.

  ZnServer startDefaultOn: 1701.
  ZnImageExampleDelegate installInDefaultServer.
  ZnServer stopDefault.

I implement the following API or resources

	GET /image - returns an HTML page showing our image and a form to upload a new image
	GET /image?raw=true - directly serves the last uploaded image
	POST /image - handler accepting a multipart form data entity with a file part containing GIF, JPEG or PNG bytes
	
Part of Zinc HTTP Components.