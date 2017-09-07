accessing
html
	^ ZnHtmlOutputStream streamContents: [ :html |
		 html page: 'Image' do: [
			html 
				tag: #src attributes: #(src 'image?raw=true');
				tag: #br;
				tag: #form attributes: #(enctype 'multipart/form-data' action image method POST) do: [
					html
						tag: #h3 with: 'Change the image:';
						tag: #input attributes: #(type file name file);
						tag: #input attributes: #(type submit value Upload) ] ] ]