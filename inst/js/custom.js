// custom textInput with limited reactivity,
// slightly modified from https://gist.github.com/xiaodaigh/7150112
var lrTextInputBinding = new Shiny.InputBinding();
              $.extend(lrTextInputBinding, {
              find: function(scope) {
              return $(scope).find('.lrTextInput');
              },
              getId: function(el) {
              //return InputBinding.prototype.getId.call(this, el) || el.name;
              return $(el).attr('id');
              },
              getValue: function(el) {
              return el.value;
              },
              setValue: function(el, value) {
              el.value = value;
              },
              subscribe: function(el, callback) {
              $(el).on('keyup.lrTextInputBinding input.lrTextInputBinding', function(event) {
              if(event.keyCode == 13) { //if enter
              callback();
              }
              });
              $(el).on('focusout.lrTextInputBinding', function(event) { // on losing focus
              callback();
              });
              },
              unsubscribe: function(el) {
              $(el).off('.lrTextInputBinding');
              },
              receiveMessage: function(el, data) {
              if (data.hasOwnProperty('value'))
              this.setValue(el, data.value);
              
              if (data.hasOwnProperty('label'))
              $(el).parent().find('label[for=' + el.id + ']').text(data.label);
              
              $(el).trigger('change');
              },
              getState: function(el) {
              return {
              label: $(el).parent().find('label[for=' + el.id + ']').text(),
              value: el.value
              };
              },
              getRatePolicy: function() {
              return {
              policy: 'debounce',
              delay: 250
              };
              }
              });
              Shiny.inputBindings.register(lrTextInputBinding, 'shiny.lrTextInput');
              Shiny.inputBindings.setPriority('shiny.lrTextInput', 10);