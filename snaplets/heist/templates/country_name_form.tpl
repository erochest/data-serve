<apply template="base">
  <div class="form-group">
    <dfForm action="/data" method="post">
      <dfChildErrorList ref=""/>

      <dfLabel ref="country">Country: </dfLabel>
      <dfInputSelect ref="country" class="form-control" style="width: 300px;"/>

      <dfInputHidden ref="page"/>
      <dfInputSubmit value="Submit" class="btn btn-default"/>
    </dfForm>
  </div>
</apply>
