publishing
publish: aPackageName dependants: dependants
	| dependantLinks |
	
	dependants ifEmpty: [ ^ self ].
	
	dependantLinks := dependants sorted collect: [ :name | '<a href="#', name , '">' , name , '</a>' ].	
	stream << 
'						<tr>
							<td id="' << aPackageName << '">' << (self htmlForPackage: aPackageName) << '</td>
							<td> ' << (self badgeFor: 'N/A') << ' </td>
							<td>' << (self sizeBadgeFor: dependants) << Character space << (Character space join: dependantLinks) << '</td>
						</tr>'; lf.