# This file is part of fortty.
# SPDX-Identifier: Apache-2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

project(
  'fortty',
  'fortran',
  version: files('version.txt'),
  license: 'Apache-2.0',
  meson_version: '>=0.57.2',
  default_options: [
    'default_library=both',
    'buildtype=debugoptimized',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')

lib_deps = []
subdir('config')

srcs = []
subdir('src')

fortty_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  dependencies: lib_deps,
  install: install,
)

fortty_inc = fortty_lib.private_dir_include()
fortty_dep = declare_dependency(
  link_with: fortty_lib,
  include_directories: fortty_inc,
  dependencies: lib_deps,
)

fortty_lic = files(
  'LICENSE',
)

if install
  # Distribute the license files in share/licenses/<name>
  install_data(
    fortty_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  module_id = meson.project_name() / fc_id + '-' + fc.version()
  meson.add_install_script(
    find_program(files('config'/'install-mod.py')),
    get_option('includedir') / module_id,
  )

  pkg = import('pkgconfig')
  pkg.generate(
    fortty_lib,
    description: 'Fortran terminal support',
    subdirs: ['', module_id],
  )
endif
