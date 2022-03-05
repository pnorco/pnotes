"use strict";

exports.focus = function(id) {
    setTimeout(()=>{ return document.getElementById(id).focus() }, 1);
};