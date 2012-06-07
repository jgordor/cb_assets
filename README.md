## Setup youcompressor

Download from:

* http://yui.zenfs.com/releases/yuicompressor/yuicompressor-2.4.7.zip


Extract and compile:

* execute ant

copy to:

* priv/yuicompressor/yuicompressor-2.4.7.jar


## Setup

Include the tag_module in your boss.config:

{template_tag_modules, [cb_assets_custom_tags]}

TODO: doc include cb_assets app for each use case

## Config sets

* priv/cb_assets.config
* Example:
[
{javascripts, [
    {sets, [
        [{name, "application.js"},
         {path, "/static/js/prod"},
         {files, ["/static/js/jquery.js", "/static/js/bootstrap.js"]}
        ],
        [{name, "web.js"},
         {path, "/static/js/prod"},
         {files, ["/static/js/jquery.js", "/static/js/bootstrap.js"]}
        ]
    ]}
]},
{stylesheets, [
    {sets, [
        [{name, "application.css"},
         {path, "/static/css/prod"},
         {files, ["/static/css/bootstrap.css", "/static/css/bootstrap-responsive.css"]}
        ],
        [{name, "web.css"},
         {path, "/static/css/prod"},
         {files, ["/static/css/web.css", "/static/css/other.css"]}
        ]
    ]}
]}
].
