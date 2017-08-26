# News

The oficial place to get news about pharo is [here](http://us11.campaign-archive1.com/home/?u=6f667565c2569234585a7be77&id=048680a940)

## 22-07-17

By Esteban Lorenzo via [Pharo-dev] Mailing list

Hi

We are releasing Pharo 6.1. 
Usually, between each major version we just apply bugfixes changing the build number and not announcing new versions but this time is different since the fixes applied required a new VM. 
The principal reason for the new version is to update Iceberg support, bringing it to macOS 64bits version. 

So, now Pharo 6.1 comes with Iceberg 0.5.5, which includes: 

- running on macOS 64bits
- adds cherry pick 
- adds major improvements on performance for big repositories
- adds pull request review plugin
- repositories browser: group branches by remote
- adds bitbucket and gitlab to recognised providers on metacello integration
- uses libgit v0.25.1 as backend
- several bugfixes

Other important change: 

- linux vm by default is now vm threaded heartbeat. 

We still miss 64bits Windows (sorry for that), but we are getting there. I hope to have it running right after ESUG.

To download 6.1 version, you can go to http://pharo.org/download page, or with zeroconf: 

wget -O- get.pharo.org | bash

Enjoy!
