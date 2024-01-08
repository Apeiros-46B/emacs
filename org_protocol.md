Only tested on Chromium. You may need to change these for Firefox.

# bookmarklet for `capture`

```js
location.href = 'org-protocol://capture?'+new URLSearchParams({template:'w',url:location.href,title:document.title,body:window.getSelection()})`
```

# bookmarklet for `store-link`

```js
location.href = 'org-protocol://store-link?'+new URLSearchParams({url:location.href,title:document.title})
```
