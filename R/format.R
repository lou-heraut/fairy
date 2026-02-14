# Copyright 2021-2026 Louis Héraut (louis.heraut@inrae.fr)*1
#
# *1   INRAE, UR RiverLy, Villeurbanne, France
#
# This file is part of fairy R package.
#
# fairy R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# fairy R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with fairy R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title clean_path
#' @description Sanitizes a character string for safe use as a path or filename.
#' @export
clean_path = function(text) {
    text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
    text = gsub("'", "_", text)
    text = gsub("[[:space:]]+", "_", text)
    text = gsub("[^a-zA-Z0-9._-]", "", text)
    text = tolower(text)
    text = gsub("^_+|_+$", "", text)
    return(text)
}


#' @title post
#' @export
post = function(x, ...) {
    if (verbose) {
        if (MPI != "") {
            print(paste0(formatC(as.character(rank),
                                 width=3, flag=" "),
                         "/", size-1, " > ", x), ...)
        } else {
            print(x, ...)
        }
    }
}
