#
# The GitHub.Team object represents a single team. It can used to
# manage users, repositories, and permissions for a team.
#
GitHub.Team: ->

GitHub.Team.prototype: {
  # Current url, duh!
  url: window.location.pathname

  # The name of this team.
  name: ->
    $('#team-name').val()

  # Boolean indicating whether this is a new, unsaved team or not.
  newRecord: ->
    # Saved teams have an ID in their URL. New teams d o not.
    not /\d/.test location.pathname

  # Adds a user or repo by login or name w ith owner depending on which
  # is passed.
  addMember: ->
    if /\//.test member
      @addRepo member
    else
      @addUser member

  # An array of string names corresponding to repos in this team.
  repos: ->
    $.makeArray $('.repositories li:visible').map ->
      $.trim $(@).find('a:first').text()

  # Accepts a string repo name. Adds a repo to this team.
  addRepo: (repo) ->
    debug "Adding repo %s", repo

    if not repo or $.inArray(repo, @repos()) > -1 then return

    @addRepoAjax repo

    li: $('.repositories').find('li:first').clone()
    input: li.find 'input[type=hidden]'

    li.find('a')
      .attr('href', '/' + repo)
      .text(repo)

    if GitHub.Autocomplete.visibilities[repo] is 'private'
      li.addClass 'private'

    if input.length > 0
      input.val(repo).attr('disabled', false)

    $('.repositories').append li.show()

    repo

  # Ajax request to add a repo to this team for reals.
  # Accepts a string reponame.
  addRepoAjax: (repo) ->
    # Don't save anything if this is a new team.
    if @newRecord() then return
    debug "Ajaxily adding %s", repo
    $.post @url + "/add_repo", { repo: repo }

  # Accepts a string reponame. Removes a repo from this team.
  removeRepo: (repo) ->
    debug "Removing %s", repo

    if not repo or $.inArray(repo, @repos()) is -1 then return

    el: $ '.repositories li:visible a:contains(' + repo + ')'
    removeLi: ->
      el.parents('li:first').remove()

    if @newRecord()
      removeLi()
    else
      el.parent().find('.remove-repository').spin().remove()
      $('#spinner').addClass('remove')
      @removeRepoAjax(repo, removeLi)

    true

  # Ajax request to remove a repo from this team for reals.
  # Accepts a string reponame.
  removeRepoAjax: (repo, callback) ->
    # Don't save anything if this is a new team.
    if @newRecord() then return
    debug "Ajaxily removing %s", repo
    $.post @url + "/remove_repo", { repo: repo }, callback

  # An array of string usernames corresponding to members of this team.
  users: ->
    $.makeArray $('.usernames li:visible').map ->
      $(@).find('a:first').text()

  # Accepts a string username. Adds a user to this team.
  addUser: (user) ->
    debug "Adding %s", user

    if not user or $.inArray(user, @users()) > -1 then return

    @addUserAjax user

    li: $('.usernames').find('li:first').clone()
    gravatar: GitHub.Autocomplete.gravatars[user]
    input: li.find 'input[type=hidden]'

    if gravatar
      li.find('img').replaceWith gravatar

    li.find('a')
      .attr('href', '/' + user)
      .text(user)

    if input.length > 0
      input.val(user).attr('disabled', false)

    $('.usernames').append li.show()

    user

  # Accepts a string username. Removes a user from this team.
  removeUser: (user) ->
    debug "Removing %s", user

    if not user or $.inArray(user, @users()) == -1 then return

    el: $ '.usernames li:visible a:contains(' + user + ')'

    removeLi: ->
      el.parents('li:first').remove()

    if @newRecord()
      removeLi()
    else
      el.parent().find('.remove-user').spin().remove()
      $('#spinner').addClass('remove')
      this.removeUserAjax(user, removeLi)

    true

  # Ajax request to add a user to this team for reals.
  # Accepts a string username.
  addUserAjax: (user) ->
    # Don't save anything if this is a new team.
    if @newRecord() then return
    debug "Ajaxily adding %s", user
    $.post @url + "/add_user", { user: user }

  # Ajax request to remove a user from this team for reals.
  # Accepts a string username.
  removeUserAjax: (user, callback) ->
    # Don't save anything if this is a new team.
    if @newRecord() then return
    debug "Ajaxily removing %s", user
    $.post @url + "/remove_user", { user: user }, callback
}

$ ->
  if $('#team-name').length is 0 then return

  team: new GitHub.Team()
  completer: new GitHub.Autocomplete()
  orgURL: team.url.split('/teams')[0]

  completer.settings.selectFirst: true
  completer.reposURL: orgURL + '/autocomplete/repos'
  completer.repos $('.add-repository-form input')

  $(".add-username-form input").userAutocomplete()

  $('.remove-repository').live 'click', ->
    team.removeRepo( $.trim($(this).prev().text()) )
    false

  $('.remove-user').live 'click', ->
    team.removeUser( $(this).prev().text() )
    false

  $(".add-username-form button, .add-repository-form button")
    .click ->
      form: $(this).parent()
      input: form.find(":text")
      member: input.val()

      debug "Clicked:"
      debug this
      debug "Adding %s", member

      if not member then return false

      input.val('').removeClass 'ac-accept'
      team.addMember member

      false

  $(".add-username-form :text, .add-repository-form :text")
    .keypress (e) ->
      if e.keyCode is $.keys.enter
        $(this).next('button').click()
        false
