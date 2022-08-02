# Purescriptify - an HTML to PureScript converter

[Try it out](https://purescriptify.web.app)

---

Turns

```html
<section class="a-class">
  <h1>Some heading</h1>
  <p>A paragraph.</p>
</section>
```

into

```purescript
module PureScript where

import React.Basic.DOM as R

node0 = R.section
  { className: "a-class"
  , children:
      [ R.h1 { children: [ R.text "Some heading" ] }
      , R.p { children: [ R.text "A paragraph." ] }
      ]
  }
```
---

### Motivation

I would find various moderate to large HTML snippets online (e.g. [Tailwind](https://tailwindcss.com/) components) and curse at how tedious they are to rewrite in PureScript. No more.

### Supported

* PureScript + React (via `React.Basic.DOM`)

### Not supported (at least yet)

* `Halogen` or other flavors
* Proper `data` and `aria` attributes handling

### Contribuing

You're very welcome.
