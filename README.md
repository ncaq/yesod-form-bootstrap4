[![Hackage](https://img.shields.io/hackage/v/yesod-form-bootstrap4.svg)](https://hackage.haskell.org/package/yesod-form-bootstrap4)
[![yesod-form-bootstrap4 on Stackage LTS](http://stackage.org/package/yesod-form-bootstrap4/badge/lts)](http://stackage.org/lts/package/yesod-form-bootstrap4)
[![test](https://github.com/ncaq/yesod-form-bootstrap4/actions/workflows/test.yml/badge.svg)](https://github.com/ncaq/yesod-form-bootstrap4/actions/workflows/test.yml)

# yesod-form-bootstrap4

This program replace [yesod-form](https://www.stackage.org/package/yesod-form) to Bootstrap v4.

# Problem

This program is not work for `boolField` and `checkBoxField`.

Because Bootstrap v4.1 checkbox layout different other.
[Forms · Bootstrap](https://getbootstrap.com/docs/4.1/components/forms/#checkboxes-and-radios)

# Workaround

use Monadic form.
and

~~~hs
fooFieldSettings :: FieldSettings master
fooFieldSettings = (bfs ("public" :: Text))
  { fsAttrs = [("class", "form-check-input")]
  }
~~~
