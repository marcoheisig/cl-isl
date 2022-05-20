%module isl

%feature("intern_function", "1");

%inline %{typedef signed char int8_t; %}
%inline %{typedef short int int16_t; %}
%inline %{typedef int int32_t; %}
%inline %{typedef long int int64_t; %}
%inline %{typedef unsigned char uint8_t; %}
%inline %{typedef unsigned short int uint16_t; %}
%inline %{typedef unsigned int uint32_t; %}
%inline %{typedef unsigned long int uint64_t; %}
%inline %{typedef long unsigned int size_t; %}
%inline %{typedef long int ssize_t; %}

%include "isl/ctx.h"
%include "isl/multi.h"
%include "isl/list.h"
%include "isl/maybe.h"
%include "isl/maybe_id.h"
%include "isl/maybe_pw_aff.h"
%include "isl/maybe_templ.h"
%include "isl/aff_type.h"
%include "isl/ast_type.h"
%include "isl/id_type.h"
%include "isl/map_type.h"
%include "isl/polynomial_type.h"
%include "isl/printer_type.h"
%include "isl/schedule_type.h"
%include "isl/set_type.h"
%include "isl/space_type.h"
%include "isl/union_map_type.h"
%include "isl/union_set_type.h"
%include "isl/val_type.h"
%include "isl/aff.h"
%include "isl/arg.h"
%include "isl/ast_build.h"
%include "isl/ast.h"
%include "isl/constraint.h"
%include "isl/fixed_box.h"
%include "isl/flow.h"
%include "isl/hash.h"
%include "isl/hmap.h"
%include "isl/id.h"
%include "isl/id_to_ast_expr.h"
%include "isl/id_to_id.h"
%include "isl/id_to_pw_aff.h"
%include "isl/ilp.h"
%include "isl/local_space.h"
%include "isl/lp.h"
%include "isl/map.h"
%include "isl/map_to_basic_set.h"
%include "isl/mat.h"
%include "isl/maybe_ast_expr.h"
%include "isl/maybe_basic_set.h"
%include "isl/obj.h"
%include "isl/options.h"
%include "isl/point.h"
%include "isl/polynomial.h"
%include "isl/printer.h"
%include "isl/schedule.h"
%include "isl/schedule_node.h"
%include "isl/set.h"
%include "isl/space.h"
%include "isl/stdint.h"
%include "isl/stream.h"
%include "isl/stride_info.h"
%include "isl/union_map.h"
%include "isl/union_set.h"
%include "isl/val_gmp.h"
%include "isl/val.h"
%include "isl/vec.h"
%include "isl/version.h"
%include "isl/vertices.h"
